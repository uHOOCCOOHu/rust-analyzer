use std::cmp::Reverse;
use std::fmt;

use crate::consteval::ComputedExpr;
use crate::db::HirDatabase;
use crate::{
    from_assoc_type_id, from_foreign_def_id, AdtId, AliasTy, Binders, ConstValue, Interner,
    Substitution, Ty,
};
use chalk_ir::{FloatTy, IntTy, Scalar, TyKind, UintTy};
use hir_def::expr::Literal;
use hir_def::{EnumId, EnumVariantId, Lookup, TypeAliasId, VariantId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyLayout {
    size: u64,
    align: u64,
    uninhabited: bool,
    sized: bool,
}

impl TyLayout {
    fn new_size_align(size: u64, align: u64) -> Self {
        Self::new_size_align_uninhabited(size, align, false)
    }

    fn new_size_align_uninhabited(size: u64, align: u64, uninhabited: bool) -> Self {
        Self { size, align, uninhabited, sized: true }
    }

    fn new_uninhabited() -> Self {
        Self { size: 0, align: 1, uninhabited: true, sized: true }
    }

    fn new_unsized() -> Self {
        Self { size: 0, align: 1, uninhabited: false, sized: false }
    }

    fn size(&self) -> u64 {
        self.size
    }

    fn align(&self) -> u64 {
        self.align
    }

    fn is_sized(&self) -> bool {
        self.sized
    }

    fn is_zst(&self) -> bool {
        self.is_sized() && self.size() == 0
    }

    fn is_uninhabited(&self) -> bool {
        self.uninhabited
    }

    #[cfg(test)]
    pub(crate) fn display_test(&self) -> impl fmt::Display + '_ {
        struct Wrapper(TyLayout);

        impl fmt::Display for Wrapper {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                if self.0.is_sized() {
                    write!(f, "size={},align={}", self.0.size(), self.0.align())?;
                    if self.0.is_uninhabited() {
                        write!(f, ",uninhabited")?;
                    }
                } else {
                    write!(f, "unsized")?;
                }
                Ok(())
            }
        }

        Wrapper(*self)
    }
}

pub(crate) fn ty_layout_query(db: &dyn HirDatabase, ty: Ty) -> Option<TyLayout> {
    let pointer_size = 8u64;

    match ty.kind(Interner) {
        TyKind::Never => Some(TyLayout::new_uninhabited()),

        TyKind::Slice(_) |
        TyKind::Str |
        TyKind::Dyn(_) =>  Some(TyLayout::new_unsized()),

        TyKind::Scalar(scalar) => {
            let (size, align) = match scalar {
                Scalar::Bool |
                Scalar::Int(IntTy::I8) | Scalar::Uint(UintTy::U8) => (1, 1),
                Scalar::Int(IntTy::I16) | Scalar::Uint(UintTy::U16) => (2, 2),
                Scalar::Char |
                Scalar::Float(FloatTy::F32) |
                Scalar::Int(IntTy::I32) | Scalar::Uint(UintTy::U32) => (4, 4),
                Scalar::Float(FloatTy::F64) |
                Scalar::Int(IntTy::I64) | Scalar::Uint(UintTy::U64) => (8, 8.min(pointer_size)),
                Scalar::Int(IntTy::I128) | Scalar::Uint(UintTy::U128) => (16, 16.min(pointer_size)),
                Scalar::Int(IntTy::Isize) | Scalar::Uint(UintTy::Usize) => (pointer_size, pointer_size),
            };
            Some(TyLayout::new_size_align(size, align))
        }
        TyKind::Array(elem, len) => {
            /*
            let len = match db.const_eval(len.interned()).ok()? {
                ComputedExpr::Literal(Literal::Int(x, _)) => u64::try_from(x).ok()?,
                _ => return None,
            };
            match db.ty_layout(*elem)? {
                TyLayout::Sized { size, align } => (size.checked_mul(len)?, align),
                TyLayout::Unsized => return None,
                TyLayout::Uninhabited => return Some(TyLayout::Uninhabited),
            }
            */
            todo!()
        }
        TyKind::Raw(_, pointee) |
        TyKind::Ref(_, _, pointee) => {
            let size = if db.ty_is_sized(pointee.clone())? {
                pointer_size
            } else {
                pointer_size * 2
            };
            Some(TyLayout::new_size_align(size, pointer_size))
        }
        TyKind::FnDef(_, _) => Some(TyLayout::new_size_align(0, pointer_size)),
        TyKind::Function(_) => Some(TyLayout::new_size_align(pointer_size, pointer_size)),

        // Not implemented.
        TyKind::Closure(_, _) |
        TyKind::Generator(_, _) |
        TyKind::GeneratorWitness(_, _) |
        // Not available.
        TyKind::InferenceVar(..) |
        TyKind::OpaqueType(..) |
        TyKind::Error |
        TyKind::Placeholder(_) => None,

        TyKind::Adt(AdtId(def_id), substs) => {
            match *def_id {
                hir_def::AdtId::StructId(id) => {
                    let field_types = db.field_types(VariantId::StructId(id));
                    let field_layouts = field_types.values().map(|ty| db.ty_layout(ty.clone().substitute(Interner, substs))).collect::<Option<Vec<_>>>()?;
                    struct_layout(db, field_layouts, None)
                }
                hir_def::AdtId::UnionId(id) => {
                    let field_types = db.field_types(VariantId::UnionId(id));
                    let field_layouts = field_types.values().map(|ty| db.ty_layout(ty.clone().substitute(Interner, substs))).collect::<Option<Vec<_>>>()?;
                    union_layout(db, field_layouts)
                }
                hir_def::AdtId::EnumId(id) => {
                    enum_layout(db, id, substs)
                }
            }
        }
        TyKind::AssociatedType(assoc_type, substs) => {
            let ty = db.ty(from_assoc_type_id(*assoc_type).into()).clone().substitute(Interner, &substs);
            db.ty_layout(ty)
        }
        TyKind::Tuple(_, substs) => {
            let field_layouts = substs.iter(Interner).filter_map(|arg| arg.ty(Interner)).map(|ty| db.ty_layout(ty.clone())).collect::<Option<Vec<_>>>()?;
            struct_layout(db, field_layouts, None)
        }
        TyKind::Foreign(foreign_id) => {
            let ty = db.ty(from_foreign_def_id(*foreign_id).into()).into_value_and_skipped_binders().0;
            db.ty_layout(ty)
        }
        TyKind::Alias(AliasTy::Projection(p_ty)) => {
            let ty = db.ty(from_assoc_type_id(p_ty.associated_ty_id).into()).clone().substitute(Interner, &p_ty.substitution);
            db.ty_layout(ty)
        }
        TyKind::Alias(AliasTy::Opaque(_)) => None,

        TyKind::BoundVar(_) => None,
    }
}

pub(crate) fn ty_is_sized_query(db: &dyn HirDatabase, ty: Ty) -> Option<bool> {
    match ty.kind(Interner) {
        TyKind::Str | TyKind::Slice(_) | TyKind::Dyn(_) => Some(false),

        TyKind::Array(_, _)
        | TyKind::Raw(_, _)
        | TyKind::Ref(_, _, _)
        | TyKind::Scalar(_)
        | TyKind::FnDef(_, _)
        | TyKind::Never
        | TyKind::Error
        | TyKind::Function(_)
        | TyKind::OpaqueType(..)
        | TyKind::Closure(..)
        | TyKind::Generator(..)
        | TyKind::GeneratorWitness(_, _) => Some(true),

        TyKind::Adt(AdtId(def_id), substs) => match *def_id {
            hir_def::AdtId::UnionId(_) | hir_def::AdtId::EnumId(_) => Some(true),
            hir_def::AdtId::StructId(id) => {
                db.field_types(VariantId::StructId(id)).values().try_fold(true, |prev, ty| {
                    Some(prev && db.ty_is_sized(ty.clone().substitute(Interner, substs))?)
                })
            }
        },
        TyKind::Tuple(_, substs) => substs
            .iter(Interner)
            .filter_map(|arg| arg.ty(Interner))
            .try_fold(true, |prev, ty| Some(prev && db.ty_is_sized(ty.clone())?)),
        TyKind::Foreign(foreign_def) => {
            let ty = db.ty(from_foreign_def_id(*foreign_def).into()).substitute(Interner, &[][..]);
            db.ty_is_sized(ty)
        }
        TyKind::AssociatedType(assoc_type, substs) => {
            let ty = db.ty(from_assoc_type_id(*assoc_type).into()).substitute(Interner, substs);
            db.ty_is_sized(ty)
        }
        TyKind::Alias(AliasTy::Projection(p_ty)) => {
            let ty = db
                .ty(from_assoc_type_id(p_ty.associated_ty_id).into())
                .substitute(Interner, &p_ty.substitution);
            db.ty_is_sized(ty)
        }
        TyKind::Alias(AliasTy::Opaque(_)) => Some(true),

        TyKind::Placeholder(_) | TyKind::BoundVar(_) | TyKind::InferenceVar(..) => None,
    }
}

fn struct_layout(
    db: &dyn HirDatabase,
    mut fields: Vec<TyLayout>,
    prefix: Option<TyLayout>,
) -> Option<TyLayout> {
    let uninhabited = fields.iter().any(TyLayout::is_uninhabited);
    let sized = fields.iter().all(TyLayout::is_sized);

    match prefix {
        Some(prefix) => {
            // Sort in ascending alignment so that the layout stay optimal
            // regardless of the prefix
            fields.sort_by_key(TyLayout::align);
            fields.insert(0, prefix);
        }
        None => {
            // Place ZSTs first to avoid "interesting offsets",
            // especially with only one or two non-ZST fields.
            fields.sort_by_key(|layout| (!layout.is_zst(), Reverse(layout.align())))
        }
    }

    let align = fields.iter().map(TyLayout::align).max().unwrap_or(1);
    let mut size = fields.iter().fold(0, |mut offset, layout| {
        let align_mask = layout.align() - 1;
        offset = (offset + align_mask) & !align_mask;
        offset += layout.size();
        offset
    });
    if size != 0 {
        let align_mask = align - 1;
        size = (size + align_mask) & !align_mask;
    }
    Some(TyLayout { size, align, uninhabited, sized })
}

fn union_layout(db: &dyn HirDatabase, fields: Vec<TyLayout>) -> Option<TyLayout> {
    if !fields.iter().all(TyLayout::is_sized) {
        return None;
    }
    if fields.is_empty() {
        return Some(TyLayout::new_uninhabited());
    }
    let uninhabited = fields.iter().all(TyLayout::is_uninhabited);
    let size = fields.iter().map(TyLayout::size).max().unwrap();
    let align = fields.iter().map(TyLayout::align).max().unwrap();
    Some(TyLayout::new_size_align_uninhabited(size, align, uninhabited))
}

fn enum_layout(db: &dyn HirDatabase, enum_id: EnumId, substs: &Substitution) -> Option<TyLayout> {
    let enum_data = db.enum_data(enum_id);

    let variant_len = enum_data.variants.len() as u64;
    if variant_len == 0 {
        return Some(TyLayout::new_uninhabited());
    }

    let pointer_size = 8;
    let prefix_layout = if variant_len == 1 {
        TyLayout::new_size_align(0, 1)
    } else if variant_len < u8::MAX.into() {
        TyLayout::new_size_align(1, 1)
    } else if variant_len < u16::MAX.into() {
        TyLayout::new_size_align(2, 2)
    } else if variant_len < u32::MAX.into() {
        TyLayout::new_size_align(4, 4)
    } else {
        TyLayout::new_size_align(8, 8.min(pointer_size))
    };

    let (mut max_size, mut max_align) = (0, 1);
    let mut uninhabited = true;
    for (variant_id, _) in enum_data.variants.iter() {
        let field_types = db.field_types(VariantId::EnumVariantId(EnumVariantId {
            parent: enum_id,
            local_id: variant_id,
        }));
        let field_layouts = field_types
            .values()
            .map(|ty| db.ty_layout(ty.clone().substitute(Interner, substs)))
            .collect::<Option<Vec<_>>>()?;
        let layout = struct_layout(db, field_layouts, Some(prefix_layout))?;
        if layout.is_uninhabited() {
            return None;
        }
        max_size = max_size.max(layout.size());
        max_align = max_align.max(layout.align());
        uninhabited &= layout.is_uninhabited();
    }
    Some(TyLayout::new_size_align_uninhabited(max_size, max_align, uninhabited))
}

pub(crate) fn ty_layout_recover(
    _db: &dyn HirDatabase,
    _cycle: &[String],
    _ty: &Ty,
) -> Option<TyLayout> {
    None
}

pub(crate) fn ty_is_sized_recover(
    _db: &dyn HirDatabase,
    _cycle: &[String],
    _ty: &Ty,
) -> Option<bool> {
    None
}

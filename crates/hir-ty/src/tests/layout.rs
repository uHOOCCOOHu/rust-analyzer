use expect_test::expect;

use super::{check, check_infer_with_mismatches, check_layout, check_no_mismatches};

#[test]
fn primitive_layout() {
    check_layout(
        r#"
//- /lib.rs
trait Trait {}
fn foo() {
    let x = loop {};
    //  ^ size=0,align=1,uninhabited
    let x: ();
    //  ^ size=0,align=1
    let x: u32;
    //  ^ size=4,align=4
    let x: (u8, u64, u32);
    //  ^ size=16,align=8
    let x: *const u8;
    //  ^ size=8,align=8
    let x: &();
    //  ^ size=8,align=8
    let x: dyn Trait;
    //  ^ unsized
}
"#,
    );
}

#[test]
fn struct_layout() {
    check_layout(
        r#"
//- /lib.rs
struct Unit;
struct Tuple(u8, u32, u8);
struct Record {
    x: !,
    y: u64,
    z: *const Struct,
    w: u8,
}
fn foo() {
    let x: Unit;
    //  ^ size=0,align=1
    let x: Tuple;
    //  ^ size=8,align=4
    let x: Record;
    //  ^ size=24,align=8,uninhabited
}
"#,
    );
}

#[test]
fn cycle_layout() {
    check_layout(
        r#"
//- /lib.rs
struct S(u8, S);
fn foo() {
    let x: S;
    //  ^ error
}
"#,
    );
}

#[test]
fn union_layout() {
    check_layout(
        r#"
//- /lib.rs
union Empty {}
union Union {
    a: (),
    b: !,
    c: u32,
    d: u64,
}
fn foo() {
    let x: Empty;
    //  ^ size=0,align=1,uninhabited
    let x: Union;
    //  ^ size=8,align=8
}
"#,
    );
}

#[test]
fn enum_layout() {
    check_layout(
        r#"
//- /lib.rs
enum Empty {}
enum One { One }
enum Two { One, Two }
enum Data {
    A,
    B(u8, u8),
    C { data: u16 },
}
enum Transparent {
    Single(Data),
}
enum Either<T> {
    Left(T),
    Right(T),
}
fn foo() {
    let x: Empty;
    //  ^ size=0,align=1,uninhabited
    let x: One;
    //  ^ size=0,align=1
    let x: Two;
    //  ^ size=1,align=1
    let x: Data;
    //  ^ size=4,align=2
    let x: Transparent;
    //  ^ size=4,align=2
    let x = Either::Left(x);
    //  ^ size=6,align=2
}
"#,
    );
}

#[test]
fn nichi_filling() {
    check_layout(
        r#"
//- minicore: option
enum Niche {
    A,
    B { x: char, y: u32 },
    C,
    D,
}
fn foo() {
    let x: Option<Option<char>>;
    //  ^ size=4,align=4
    let x: Option<&()>
    //  ^ size=8,align=8
    let x: Option<Option<&()>>
    //  ^ size=16,align=8
    let x: Result<&(), ()>,
    //  ^ size=8,align=8
    let x: Niche,
    //  ^ size=8,align=4
}
"#,
    );
}

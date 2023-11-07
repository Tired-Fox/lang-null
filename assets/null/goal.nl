// Cannot re-assign value and cannot be changed. Allowed in global scope

// Types:
// i8 i16 i32 i64 i128 (int == ptr to the best fit size)
// u8 u16 u32 u64 u128 (uint == ptr to the best fit size)
//    f16 f32 f64      (float == ptr to the best fit size)
// string
// any
// undefined

x: int : 0;
x :: 0
const x: int = 0;
x: i32 = 0;
x := 0
let x: i32 = 0;

/*
    Multiline comment `/* markdown code escapes closing symbols */`
*/

/**
  Doc comment: Supports markdown syntax
*/
pub enum Weekday {
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
    Sunday,
}

pub union Option {
    Day: Weekday,
    Week: u32,
    Month: u8,
    Year: u16
}

pub enum Error {
    UnkownError,
    OptionError(String)
}

union_enum :: fn() Error!?String {
    // Can re-assign value
    let x: i32 = 0;
    
    // Can mutate value
    let mut x: i32 = 0;
    let my_option = Option::Day(Weekday::Monday)
}

pub template SayHello {
    say_hello::fn(self: &Self)
}

struct Data {
    name: String

    _::fn(self) {
        /* Deconstructor: Called on dropping self */
    }

    name::fn(&self) &String {
        &self.name
    }

    hello::fn() {
        print("Hello")
    }
}

extend Data with SayHello {
    say_hello::fn(&self) {
        let suffix = '!'
        print("Hello, {}{}", self.name, suffix)
    }
}

math :: fn(name: String, ..args: []String) {
  let result: int = 13*4+6/5%((4**4)//3)-3;

  if true and false or not false {
  }
}

optional :: (code?: i32) bool {
    false
}

main :: fn() {
    print("Hello, world!");
    print("Hello, {}!", Sub::NAME);
    let value: []string = [];
    let slice: &[]string = value[..3];
}

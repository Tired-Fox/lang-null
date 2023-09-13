// Cannot re-assign value and cannot be changed. Allowed in global scope
const x: i32 = 0;

/*
    Multiline comment
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

union_enum :: () Error!?String {
    // Can re-assign value
    let x: i32 = 0;
    
    // Can mutate value
    let mut x: i32 = 0;
    let my_option = Option::Day(Weekday::Monday)
}

pub template SayHello {
    say_hello::(&self)
}

struct Data {
    name: String

    _::(self) {
        /* Deconstructor: Called on dropping self */
    }

    name::(&self) &String {
        &self.name
    }

    hello::() {
        print("Hello")
    }
}

extend Data with SayHello {
    say_hello::(&self) {
        let suffix = '!';
        print("Hello, {}{}", self.name, suffix)
    }
}

math::(...args: []String, name: String) {
  let result: i64 = 13*4+6/5%((4**4)//3)-3;

  if true and false or not false {
  }
}

main::() {
    print("Hello, world!");
    print("Hello, {}!", Sub::NAME);
    let value: []String = [];
    let slice: &[]String = value[..3];
}

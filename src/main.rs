use std::env;
use std::fs::File;
use std::io::{ self, Stdout, Stdin, Read, Write };
use std::ops::{ Index, IndexMut };

trait Console {
    fn input(&mut self) -> Platter;
    fn output(&mut self, character: Platter);
}
trait Scroll {
    fn into_array(self) -> Array;
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
struct Platter(u32);

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
enum Operator {
    ConditionalMove(u32, u32, u32),
    ArrayIndex(u32, u32, u32),
    ArrayAmendment(u32, u32, u32),
    Addition(u32, u32, u32),
    Multiplication(u32, u32, u32),
    Division(u32, u32, u32),
    NotAnd(u32, u32, u32),
    Halt,
    Allocation(u32, u32),
    Abandonment(u32),
    Output(u32),
    Input(u32),
    LoadProgram(u32, u32),
    Orthography(u32, u32),
}

#[derive(Clone, Debug)]
struct Array(Vec<Platter>);

#[derive(Debug)]
struct Arrays(Vec<Array>);

#[derive(Debug)]
struct Registers([Platter; 8]);

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
struct Finger {
    array: Platter,
    platter: Platter,
}

#[derive(Debug)]
struct Machine<C: Console> {
    registers: Registers,
    arrays: Arrays,
    execution_finger: Finger,
    console: C,
    halted: bool,
}

#[derive(Debug)]
struct LegacyScroll(Vec<u8>);

impl Platter {
    fn zero() -> Platter {
        Platter(0)
    }
}

impl From<Platter> for Operator {
    fn from(platter: Platter) -> Operator {
        let operator_number = platter.0 >> 28;
        let (a, b, c) = (((platter.0 >> 6) & 0b111), ((platter.0 >> 3) & 0b111), (platter.0 & 0b111));
        match operator_number {
            0 => Operator::ConditionalMove(a, b, c),
            1 => Operator::ArrayIndex(a, b, c),
            2 => Operator::ArrayAmendment(a, b, c),
            3 => Operator::Addition(a, b, c),
            4 => Operator::Multiplication(a, b, c),
            5 => Operator::Division(a, b, c),
            6 => Operator::NotAnd(a, b, c),
            7 => Operator::Halt,
            8 => Operator::Allocation(b, c),
            9 => Operator::Abandonment(c),
            10 => Operator::Output(c),
            11 => Operator::Input(c),
            12 => Operator::LoadProgram(b, c),
            13 => Operator::Orthography(((platter.0 >> 25) & 0b111), (platter.0 & ((1 << 25) - 1))),
            _ => panic!("Unknown operator number {}", operator_number),
        }
    }
}

impl Scroll for LegacyScroll {
    fn into_array(self) -> Array {
        Array(self.0.chunks(4).map(|abcd| Platter(((abcd[0] as u32) << 24) + ((abcd[1] as u32) << 16) + ((abcd[2] as u32) << 8) + (abcd[3] as u32))).collect())
    }
}

impl Array {
    fn abandon(&mut self) {
        self.0.clear()
    }

    fn allocate(size: Platter) -> Array {
        Array(vec![Platter::zero(); size.0 as usize])
    }
}

impl Index<Platter> for Array {
    type Output = Platter;
    fn index(&self, i: Platter) -> &Platter {
        &self.0[i.0 as usize]
    }
}

impl IndexMut<Platter> for Array {
    fn index_mut(&mut self, i: Platter) -> &mut Platter {
        &mut self.0[i.0 as usize]
    }
}

impl Arrays {
    fn allocate(&mut self, size: Platter) -> Platter {
        let i = Platter(self.0.len() as u32);
        self.0.push(Array::allocate(size));
        i
    }
}

impl Index<Platter> for Arrays {
    type Output = Array;
    fn index(&self, i: Platter) -> &Array {
        &self.0[i.0 as usize]
    }
}

impl IndexMut<Platter> for Arrays {
    fn index_mut(&mut self, i: Platter) -> &mut Array {
        &mut self.0[i.0 as usize]
    }
}

impl Index<u32> for Registers {
    type Output = Platter;
    fn index(&self, i: u32) -> &Platter {
        &self.0[i as usize]
    }
}

impl IndexMut<u32> for Registers {
    fn index_mut(&mut self, i: u32) -> &mut Platter {
        &mut self.0[i as usize]
    }
}

impl Finger {
    fn advance(&mut self) {
        self.platter = Platter(self.platter.0 + 1);
    }
}

impl<C: Console> Machine<C> {
    fn new<S: Scroll>(program: S, console: C) -> Machine<C> {
        Machine {
            registers: Registers([Platter::zero(); 8]),
            arrays: Arrays(vec![program.into_array()]),
            execution_finger: Finger {
                array: Platter::zero(),
                platter: Platter::zero(),
            },
            console: console,
            halted: false,
        }
    }

    fn spin_cycle(&mut self) {
        if self.halted { return }
        let operator = Operator::from(self.get_platter(self.execution_finger));
        self.execution_finger.advance();
        self.discharge(operator);
    }

    fn get_platter(&self, finger: Finger) -> Platter {
        self.arrays[finger.array][finger.platter]
    }

    fn discharge(&mut self, operator: Operator) {
        match operator {
            Operator::ConditionalMove(a, b, c) => {
                if self.registers[c] != Platter::zero() {
                    self.registers[a] = self.registers[b];
                }
            }
            Operator::ArrayIndex(a, b, c) => {
                self.registers[a] = self.arrays[self.registers[b]][self.registers[c]];
            }
            Operator::ArrayAmendment(a, b, c) => {
                self.arrays[self.registers[a]][self.registers[b]] = self.registers[c];
            }
            Operator::Addition(a, b, c) => {
                self.registers[a] = Platter(self.registers[b].0.wrapping_add(self.registers[c].0));
            }
            Operator::Multiplication(a, b, c) => {
                self.registers[a] = Platter(self.registers[b].0.wrapping_mul(self.registers[c].0));
            }
            Operator::Division(a, b, c) => {
                self.registers[a] = Platter(self.registers[b].0 / self.registers[c].0);
            }
            Operator::NotAnd(a, b, c) => {
                self.registers[a] = Platter(!self.registers[b].0 | !self.registers[c].0);
            }
            Operator::Halt => {
                self.halted = true;
            }
            Operator::Allocation(b, c) => {
                self.registers[b] = self.arrays.allocate(self.registers[c]);
            }
            Operator::Abandonment(c) => {
                self.arrays[self.registers[c]].abandon();
            }
            Operator::Output(c) => {
                self.console.output(self.registers[c]);
            }
            Operator::Input(c) => {
                self.registers[c] = self.console.input();
            }
            Operator::LoadProgram(b, c) => {
                if self.registers[b] != Platter::zero() {
                    self.arrays[Platter::zero()] = self.arrays[self.registers[b]].clone();
                }
                self.execution_finger.platter = self.registers[c];
            }
            Operator::Orthography(a, value) => {
                self.registers[a] = Platter(value);
            }
        }
    }
}

struct StdConsole(Stdout, Stdin);
impl Console for StdConsole {
    fn input(&mut self) -> Platter {
        let mut buf = [0];
        if self.1.read(&mut buf).unwrap() == 1 {
            Platter(buf[0] as u32)
        } else {
            Platter(0xFFFFFFFF)
        }
    }

    fn output(&mut self, character: Platter) {
        let buf = [character.0 as u8];
        self.0.write(&buf).unwrap();
    }
}

fn main() {
    let file = env::args().skip(1).next().unwrap();
    let mut bytes = Vec::new();
    File::open(file).unwrap().read_to_end(&mut bytes).unwrap();
    let mut machine = Machine::new(LegacyScroll(bytes), StdConsole(io::stdout(), io::stdin()));
    while !machine.halted {
        machine.spin_cycle();
    }
}


fn make-stack[] {
  stack[#nil]
}

fn stack[state] { obj {
| @push x . #cons [%x, %state]
| @pop . match %state {
  | #nil . #empty
  | #cons [x, state] . #some [%x, stack[%state]]
  }
| @peek-top . match %state {
  | #nil . [#nil, stack[#nil]]
  | #cons [x, state] . [clone x, stack[#cons[%x, %state]]]
  }
}}


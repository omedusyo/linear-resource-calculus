
fn interval[a, b] { match <=[clone a, clone b] {
| #T . #interval [%a, %b]
| #F . #interval [%b, %a]
}}


// Newton's method:
//fn improve[guess, x] {
//  div[+[ clone guess, div[%x, %guess] ], ◊f32{2.0}]
//}


//{
    sqrt(x) < mid
  iff
    x < mid*mid
}
fn improve[interval, x] { match %interval {
| #interval [a, b] . let {
  , mid = div[+[clone a, clone b], ◊f32{2.0}] . let {
  , mid-square = *[clone mid, clone mid]
  . match <=[%mid-square, %x] {
    | #T . drop a . interval[%mid, %b]
    | #F . drop b . interval[%a, %mid]
    }
  }}
}}

fn sqrt[x] { sqrt-object[interval[◊f32{1.0}, clone x], %x] }

fn sqrt-object[interval, x] { obj {
// | @get . [clone interval, sqrt-object[%guess, %x]]
| @improve . sqrt-object[improve[%interval, clone x], %x]
}}


fn good-enough?[guess, x, epsilon] {
  <=[distance[*[clone guess, %guess], %x], %epsilon]
}

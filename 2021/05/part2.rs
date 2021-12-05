use std::io::{self, BufRead};
use std::cmp;
use itertools::Itertools;
use std::collections::HashSet;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i16,
    y: i16,
}


#[derive(Debug)]
struct Vent {
    points: HashSet<Point>,
}


fn div(a: i16, b: i16) -> i16 {
    if b == 0 {
        0
    } else {
        a / b
    }
}


impl Vent {
    pub fn new(start: Point, end: Point) -> Vent {
        let change_x = end.x - start.x;
        let change_y = end.y - start.y;
        let max_change = cmp::max(change_x.abs(), change_y.abs());

        Vent {
            points: (0..=max_change)
                .map(|i| {
                    Point {
                        x: start.x + (i * div(change_x, max_change.abs())),
                        y: start.y + (i * div(change_y, max_change.abs())),
                    }
                }).collect()
        }
    }
}


fn combine<A, B, C, D, E>((a1, b1, c1, d1): (Result<A, E>, Result<B, E>, Result<C, E>, Result<D, E>)) -> Result<(A, B, C, D), E> {
    match (a1, b1, c1, d1) {
        (Ok(a2), Ok(b2), Ok(c2), Ok(d2)) => { Ok((a2, b2, c2, d2)) },
        (Err(a3), _, _, _) => { Err(a3) },
        (_, Err(b3), _, _) => { Err(b3) },
        (_, _, Err(c3), _) => { Err(c3) },
        (_, _, _, Err(d3)) => { Err(d3) },
    }
}


fn main() {
    let mut vents: Vec<Vent> = Vec::new();
    let stdin = io::stdin();

    // Read in vents
    for line_ in stdin.lock().lines() {
        match line_ {
            Ok(line) => {
                match line.replace(" -> ", ",").split(",").collect::<Vec<&str>>()[..] {
                    [x1, y1, x2, y2] => {
                        let int_positions = (
                            x1.parse::<i16>(),
                            y1.parse::<i16>(),
                            x2.parse::<i16>(),
                            y2.parse::<i16>(),
                        );

                        match combine(int_positions) {
                            Ok((x1, y1, x2, y2)) => {
                                vents.push(Vent::new(
                                    Point { x: x1, y: y1 },
                                    Point { x: x2, y: y2 },
                                ));
                            },

                            Err(err) => {
                                println!("{}", err);
                            },
                        };
                    },

                    _ => {
                        println!("unrecognisable line");
                    },
                };
            },

            Err(err) => {
                println!("{}", err);
            },
        };
    }

    // Intersect points
    let mut peekable_vents = vents.iter().multipeek();
    let mut all_intersections: HashSet<Point> = HashSet::new();

    while let Some(current_vent) = peekable_vents.next() {
        while let Some(next_vent) = peekable_vents.peek() {
            for intersecting_point in current_vent.points.intersection(&next_vent.points) {
                all_intersections.insert(*intersecting_point);
            }
        }
    }

    println!("{:?}", all_intersections.iter().count());
}

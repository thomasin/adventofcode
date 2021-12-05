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


impl Vent {
    pub fn new(start: Point, end: Point) -> Option<Vent> {
        if start.x == end.x && start.y == end.y {
            Some(Vent {
                points: HashSet::from([Point { x: start.x, y: start.y }]),
            })
        } else if start.x == end.x {
            let all_y = cmp::min(start.y, end.y)..=cmp::max(start.y, end.y);

            Some(Vent {
                points: all_y.map(|y| Point { x: start.x, y: y }).collect(),
            })
        } else if start.y == end.y {
            let all_x = cmp::min(start.x, end.x)..=cmp::max(start.x, end.x);

            Some(Vent {
                points: all_x.map(|x| Point { x: x, y: start.y }).collect(),
            })
        } else {
            None
        }
    }

    pub fn intersections_with(&self, vent: &Vent) -> HashSet<Point> {
        let mut intersection = HashSet::new();

        for intersecting_point in self.points.intersection(&vent.points) {
            intersection.insert(*intersecting_point);
        };

        intersection 
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
                                match Vent::new(Point { x: x1, y: y1 }, Point { x: x2, y: y2 }) {
                                    Some(vent) => {
                                        vents.push(vent);
                                    },

                                    None => {
                                    },
                                }
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

    let mut peekable_vents = vents.iter().multipeek();
    let mut all_intersections: HashSet<Point> = HashSet::new();

    while let Some(current_vent) = peekable_vents.next() {
        while let Some(next_vent) = peekable_vents.peek() {
            for intersection in current_vent.intersections_with(next_vent) {
                all_intersections.insert(intersection);
            }
        }
    }

    println!("{:?}", all_intersections.iter().count());
}

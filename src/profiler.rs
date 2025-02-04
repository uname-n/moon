use std::time::Instant;

pub fn profile<F, R>(f: F) -> (R, u128)
where
    F: FnOnce() -> R,
{
    let start = Instant::now();
    let result = f();
    let duration = start.elapsed().as_micros();
    (result, duration)
}
// build.rs

fn main() {
    let date = chrono::Utc::now();
    println!("cargo:rustc-env=DATE={}", date.format("%A %B %e %Y"));
}

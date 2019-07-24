use std::io;
use rand::Rng;
use std::cmp::Ordering;

fn main() {
    let secret_number = rand::thread_rng().gen_range(1,101);
    println!("Guess the number!");

    loop {
        let guess = guess();
        match guess.cmp(&secret_number) {
            Ordering::Less => println!("Too small!"),
            Ordering::Equal => {
                println!("You win!");
                return;
            }
            Ordering::Greater => println!("Too big!")
        }
    }
}

fn guess() -> i32 {
    println!("Please input your guess.");
    let mut guess = String::new();
    io::stdin().read_line(&mut guess).expect("Failed to read line");

    println!("You guessed: {}", guess);

    guess.trim().parse::<i32>().unwrap()
}
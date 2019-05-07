#![feature(test)]
#![feature(plugin)]
#![plugin(afl_coverage_plugin)]

extern crate afl;
extern crate test;
extern crate gif;

use gif::SetParameter;
use std::io::{self, Read};

fn main() {
    let mut input = Vec::new();
    io::stdin().read_to_end(&mut input).unwrap();
    let mut decoder = gif::Decoder::new(&*input);
    /*let file = ::std::fs::File::open(
        "sync_dir/fuzzer01/hangs/id:000000,src:000000,op:havoc,rep:64"
    ).unwrap();
    let mut decoder = gif::Decoder::new(file);*/
    decoder.set(gif::ColorOutput::RGBA);
    match (|| -> Result<(), gif::DecodingError> {
        let mut decoder = decoder.read_info()?;
        println!("width = {}, height = {}", decoder.width(), decoder.height());
        let mut i = 0;
        loop {
            if let Some(frame) = decoder.next_frame_info()? {
                i += 1;
                println!("frame {}: {:?}", i, frame);
            } else { break }
            let mut vec = vec![0; decoder.buffer_size()];
            decoder.fill_buffer(&mut vec)?;
            test::black_box(vec);
        }
        Ok(())
    })() {
        Ok(_) => (),
        Err(err) => println!("{:?}", err)
    }
}
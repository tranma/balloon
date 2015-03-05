# Balloon

Balloon flying on Earth with severe climate change.

## Random Generator

Generate random data:

```
$ ./dist/build/random/random <number-of-seconds-you-would-like-to-wait> <output-file>
```

On my 2014 MBP, this generates about 60000 lines of sample data per second.

## Compute Stats

Compute some basic stats on some data.

```
$ ./dist/build/balloon/balloon
Usage: balloon (-f|--file ARG) [-a|--max] [-i|--min] [-e|--mean] [-c|--count] [-d|--dist]
```

Example of processing about 13MB of data:

```
$ wc -l samples
    411911 samples

$ time ./dist/build/balloon/balloon -f samples -a -i -e -c -d +RTS -N1
running on 1 threads
6.00s user 0.33s system 99% cpu 6.377 total

$  time ./dist/build/balloon/balloon -f samples -a -i -e -c -d +RTS -N4
running on 4 threads
9.87s user 1.62s system 327% cpu 3.504 total
```

## Normalisation

Normalise data to desired units.

```
./dist/build/normal/normal
Usage: normal (-i|--in ARG) (-o|--out ARG) [-t|--temp ARG] [-d|--dist ARG]
```

Temparatures: `C`, `F`, `K`. Distance: `KM`, `M`, `ML`.
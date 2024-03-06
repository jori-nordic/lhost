# LHost: Toy bluetooth host in common lisp

## What is it?

A toy implementation of the Bluetooth LE host in common lisp.
It's really broken, please don't use it.

For now, it talks to a virtualized controller that uses the [Babblesim](https://babblesim.github.io/#content) simulator.

Communication with the controller device is via a Zephyr UART driver that goes
over unix pipes. 
The driver is in this repo, under `drivers`, but should be upstreamed soon.

## Why???

- I like image-based development
- I want to quickly experiment ideas for [my day job](https://github.com/jori-nordic)
- Dead languages don't have painful updates
- I want a job at google, they seem to love rewriting BT hosts

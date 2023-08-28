# About

This is an Erlang web application using the [Nitrogen Web Framework](https://nitrogenproject.com).

I rewrote this project out of curiosity to check the differences between writing in elixir and erlang.
The main functions of the application are located in **/src/core**, and the Nitrogen part responsible for the frontend lies in **/src/index.erl**.

[Elixir version of project](https://github.com/lucas0dev/sudoku_solver).


# Building

You can rebuild the project by typing:

   make

If this fails, it likely means that its a slim release and Erlang is not
installed on this machine. Please [install
Erlang](https://www.erlang-solutions.com/resources/download.html).

# Running

You can start the application with:

  **make run_dev**  
  or
  **make run_release**


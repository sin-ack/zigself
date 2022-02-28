(|
    "This should fail"
    someMethod: arg = (arg value printLine).

    run = (| value |
        value: 'This should not print!'.
        someMethod: self.
    ).

|) run.

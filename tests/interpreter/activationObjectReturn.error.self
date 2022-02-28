(|
    methodThatReturnsSelf = (| value |
        value: 'This should not print!'.
        self
    ).

    "This should fail"
    run = (
        methodThatReturnsSelf value printLine.
    ).
|) run.

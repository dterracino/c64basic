module System

type IScreen = interface
    abstract member WriteLine : string -> unit
end

type ConsoleScreen() =
    interface IScreen with
        member this.WriteLine text =
            System.Console.WriteLine text

type Kernel(screen : IScreen) =
    member this.Screen = screen

let createKernel =
    Kernel(ConsoleScreen())
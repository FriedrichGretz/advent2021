type SnailNumber =
    {
        f: NumOrPair
        s: NumOrPair
    }
    override this.ToString() =
        sprintf "[%s, %s]" (this.f.ToString()) (this.s.ToString())
and NumOrPair =
    | Num of int
    | Pair of SnailNumber
    override this.ToString() =
        match this with
        | Num x -> string x
        | Pair sn -> sn.ToString()


let rec reduceSN n =
    

let addSN a b =
    { f = Pair a
      s = Pair b }
    |> reduceSN
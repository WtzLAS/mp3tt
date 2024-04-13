package mp3tt.model.debruijn

type Idx = Int

type Lvl = Int

extension (x: Lvl) def toIdx(l: Lvl): Idx = l - x - 1

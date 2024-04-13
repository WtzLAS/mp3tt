package mp3tt.model.debruijn

opaque type Idx = Int

extension [T](s: IndexedSeq[T]) def apply(idx: Idx) = s(idx)

opaque type Lvl = Int

extension (x: Lvl)
  def toIdx(l: Lvl): Idx = l - x - 1
  def extended: Lvl = x + 1

extension [T](s: IndexedSeq[T]) def lvl: Lvl = s.length
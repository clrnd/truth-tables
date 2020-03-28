module Row.Class where


import Prim.Row (class Union)


class SubRow (child :: # Type) (parent :: # Type)

instance subRowViaUnion :: Union child any parent => SubRow child parent


class IntersectRow (leftRow :: # Type) (rightRow :: # Type) (intersection :: # Type) (leftRowRemainder :: # Type) (rightRowRemainder :: # Type)

instance intersectRowViaUnion
      :: ( Union leftRowRemainder intersection leftRow
         , Union rightRowRemainder intersection rightRow
         ) => IntersectRow leftRow rightRow intersection leftRowRemainder rightRowRemainder

# clj-chess

Chess implementation in clojure. Game rendering via [quil](https://github.com/quil/quil)

# TODO
- [x] pawn moves
- [x] pawn attacks
- [x] board render (quil)
- [x] pawn coll render (quil)
- [x] pawn moving by dragging (quil)
- [x] convert the mouse position to the cell coord
- [x] highlight the cell on hover
- [x] can move a piece
- [x] pawn moving by allowing to teleport it on one of the allowed squares (quil)
- [x] first move of a pawn can be a leap
- [x] do not cover the piece with the selection square
- [x] make the selected piece selection square a different color
- [x] bug: pawn can jump over enemy piece
- [x] add attacks
- [x] add capturing the piece
- [x] combine drawing attacks and moves in one let
- [x] reconsider how the state works
- [x] add king
- [x] add rook
- [x] add bishop
- [x] add knight
- [x] add queen
- [x] check functionality
- [x] mate functionality
- [x] use the pre-calculated moves/attacks in the renderer
- [x] turn switching
- [x] render a side panel with game info
- [x] add a visual of captured pieces in the side panel
    + [x] render little icons of the chess pieces
    + [x] sort the icons (alphabetically is fine)
- [ ] pawn promotion
- [ ] implement en-passant
- [ ] implement castling
- [ ] game over functionality

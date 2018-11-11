module Env = Reprocessing.Env;
module Draw = Reprocessing.Draw;
module Utils = Reprocessing.Utils;

let pointSize = 30;
let boardWidth = 10;
let boardHeight = 16;
let stepInterval = 0.8;

let grayBackground = Utils.color(~r=33, ~g=37, ~b=41, ~a=255);
let darkGrayStroke = Utils.color(~r=52, ~g=58, ~b=64, ~a=255);

type point = (int, int);

/* range(3) -> [0, 1, 2] */
let range = max => {
  let rec aux = (acc, n) =>
    switch (n) {
    | 0 => [0, ...acc]
    | n => aux([n - 1, ...acc], n - 1)
    };
  aux([], max);
};

module Piece = {
  type orientation =
    | Up
    | Down
    | Left
    | Right;

  type t =
    | I
    | O
    | T
    | S
    | Z
    | J
    | L;

  let random = () =>
    switch (Random.int(7)) {
    | 0 => I
    | 1 => O
    | 2 => T
    | 3 => S
    | 4 => Z
    | 5 => J
    | 6 => L
    | _ => failwith("lol")
    };

  let color = piece =>
    switch (piece) {
    | I => Utils.color(~r=92, ~g=124, ~b=250, ~a=255)
    | O => Utils.color(~r=252, ~g=196, ~b=25, ~a=255)
    | T => Utils.color(~r=132, ~g=94, ~b=247, ~a=255)
    | S => Utils.color(~r=81, ~g=207, ~b=102, ~a=255)
    | Z => Utils.color(~r=255, ~g=107, ~b=107, ~a=255)
    | J => Utils.color(~r=204, ~g=93, ~b=232, ~a=255)
    | L => Utils.color(~r=255, ~g=146, ~b=43, ~a=255)
    };

  /* You'll have to implement orientation lol */
  let draw = (~piece, ~orientation, ~point, env) => {
    let (x, y) = point;
    let shape =
      switch (piece, orientation) {
      | (I, Left)
      | (I, Right) => [[1, 1, 1, 1]]
      | (I, Up)
      | (I, Down) => [[1], [1], [1], [1]]
      | (O, _) => [[1, 1], [1, 1]]
      | (T, Down) => [[0, 1, 0], [1, 1, 1]]
      | (T, Up) => [[1, 1, 1], [0, 1, 0]]
      | (T, Left) => [[0, 1], [1, 1], [0, 1]]
      | (T, Right) => [[1, 0], [1, 1], [1, 0]]
      | (S, Up)
      | (S, Down) => [[0, 1, 1], [1, 1, 0]]
      | (S, Left)
      | (S, Right) => [[0, 1, 1], [1, 1, 0]]
      | (Z, Up)
      | (Z, Down) => [[1, 1, 0], [0, 1, 1]]
      | (Z, Left)
      | (Z, Right) => [[0, 1], [1, 1], [1, 0]]
      | (J, Right) => [[1, 0, 0], [1, 1, 1]]
      | (J, Left) => [[1, 1, 1], [0, 0, 1]]
      | (J, Up) => [[0, 1], [0, 1], [1, 1]]
      | (J, Down) => [[1, 1], [1, 0], [1, 0]]
      | (L, Left) => [[0, 0, 1], [1, 1, 1]]
      | (L, Right) => [[1, 1, 1], [1, 0, 0]]
      | (L, Up) => [[1, 0], [1, 0], [1, 1]]
      | (L, Down) => [[1, 1], [0, 1], [0, 1]]
      };
    let rowCount = ref(0);
    let columnCount = ref(0);
    List.iter(
      row => {
        List.iter(
          column => {
            if (column === 1) {
              Draw.fill(color(piece), env);
              Draw.rect(
                ~pos=(
                  (x + columnCount^) * pointSize,
                  (y + rowCount^) * pointSize,
                ),
                ~width=pointSize,
                ~height=pointSize,
                env,
              );
            };
            columnCount := columnCount^ + 1;
          },
          row,
        );
        rowCount := rowCount^ + 1;
        columnCount := 0;
      },
      shape,
    );
  };

  let getWidth = (piece, orientation): int =>
    switch (piece, orientation) {
    | (I, Up)
    | (I, Down) => 1
    | (I, Left)
    | (I, Right) => 4
    | (O, _) => 2
    | (T, Up)
    | (T, Down) => 3
    | (T, Left)
    | (T, Right) => 2
    | (S, Up)
    | (S, Down) => 3
    | (S, Left)
    | (S, Right) => 2
    | (Z, Up)
    | (Z, Down) => 3
    | (Z, Left)
    | (Z, Right) => 2
    | (J, Up)
    | (J, Down) => 2
    | (J, Left)
    | (J, Right) => 3
    | (L, Up)
    | (L, Down) => 2
    | (L, Left)
    | (L, Right) => 3
    };

  let getHeight = (piece, orientation): int =>
    switch (orientation) {
    | Up
    | Down => getWidth(piece, Right)
    | Left
    | Right => getWidth(piece, Down)
    };
};

type status =
  | Playing
  | GameOver;

type tile =
  | Blank
  | Filled(Reprocessing.colorT);

type state = {
  status,
  stepTimer: float,
  currentPiece: (Piece.t, Piece.orientation, point),
  board: list(list(tile)),
};

module Board = {
  let make = () => {
    let rows = range(boardHeight);
    let columns = range(boardWidth);
    List.map(_ => List.map(_ => Blank, columns), rows);
  };

  let draw = (state, env) => {
    let rowCount = ref(0);
    let columnCount = ref(0);
    List.iter(
      row => {
        List.iter(
          tile => {
            switch (tile) {
            | Blank => Draw.fill(grayBackground, env)
            | Filled(color) => Draw.fill(color, env)
            };
            Draw.stroke(darkGrayStroke, env);
            Draw.rect(
              ~pos=(columnCount^ * pointSize, rowCount^ * pointSize),
              ~width=pointSize,
              ~height=pointSize,
              env,
            );
            columnCount := columnCount^ + 1;
          },
          row,
        );
        rowCount := rowCount^ + 1;
        columnCount := 0;
      },
      state.board,
    );
  };

  /* You'll need to do a lot of stuff in here */
  let next = state => (state.currentPiece, state.board);
};

let setup = env => {
  Env.size(
    ~width=boardWidth * pointSize,
    ~height=boardHeight * pointSize,
    env,
  );
  {
    status: Playing,
    stepTimer: 0.0,
    currentPiece: (Piece.random(), Piece.Up, (0, 0)),
    board: Board.make(),
  };
};

let stepPiece = (isNextStep, state) => {
  let (piece, orientation, point) = state.currentPiece;
  let (x, y) = point;
  isNextStep ? (piece, orientation, (x, y + 1)) : state.currentPiece;
};

let draw = (state, env) => {
  Draw.background(grayBackground, env);
  Board.draw(state, env);

  let newStepTimer = state.stepTimer +. Env.deltaTime(env);
  let isNextStep = newStepTimer > stepInterval;
  let stepTimer = isNextStep ? 0.0 : newStepTimer;

  let (p, orientation, (_, y)) = state.currentPiece;
  let currentY = Piece.getHeight(p, orientation) + y;
  let isReachingBottom = currentY == boardHeight;

  let (nextPiece, nextBoard) =
    Board.next({
      ...state,
      currentPiece:
        isReachingBottom ? state.currentPiece : stepPiece(isNextStep, state),
    });
  let (piece, orientation, point) = nextPiece;
  Piece.draw(~piece, ~orientation, ~point, env);

  {...state, stepTimer, currentPiece: nextPiece, board: nextBoard};
};

module Direction = {
  type t =
    | Left
    | Right
    | Down;
};

let moveLeft = piece => {
  let (p, orientation, (x, y)) = piece;
  x <= 0 ? piece : (p, orientation, (x - 1, y));
};

let moveRight = piece => {
  let (p, orientation, (x, y)) = piece;
  x + Piece.getWidth(p, orientation) >= boardWidth ?
    piece : (p, orientation, (x + 1, y));
};

let keyPressed = (state, env) =>
  switch (Env.keyCode(env)) {
  | Up => state
  | Down => state
  | Left => {...state, currentPiece: moveLeft(state.currentPiece)}
  | Right => {...state, currentPiece: moveRight(state.currentPiece)}
  | _ => state
  };

Reprocessing.run(~setup, ~draw, ~keyPressed, ());

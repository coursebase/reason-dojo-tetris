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

  let getShape = (piece, orientation) =>
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
    | (S, Right) => [[1, 0], [1, 1], [0, 1]]
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

  let getPoints = (piece, orientation, point) => {
    let (x, y) = point;
    let shape = getShape(piece, orientation);
    let points = ref([]);
    List.iteri(
      (idx, sublist) =>
        List.iteri(
          (idx2, item) =>
            if (item == 1) {
              points := [(x + idx2, y + idx), ...points^];
            },
          sublist,
        ),
      shape,
    );
    points^;
  };

  /* You'll have to implement orientation lol */
  let draw = (~piece, ~orientation, ~point, env) => {
    let (x, y) = point;
    let shape = getShape(piece, orientation);
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
  pastPieces: list((Piece.t, Piece.orientation, point)),
  collisionPoints: list(point),
  board: list(list(tile)),
};

let stepPiece = state => {
  let (piece, orientation, point) = state.currentPiece;
  let (x, y) = point;
  (piece, orientation, (x, y + 1));
};

let makeRandomPiece = () => {
  let getRandomOrientation = () =>
    switch (Random.int(4)) {
    | 0 => Piece.Up
    | 1 => Piece.Down
    | 2 => Piece.Left
    | 3 => Piece.Right
    | _ => Piece.Up
    };
  let piece = Piece.random();
  (
    piece,
    getRandomOrientation(),
    (boardWidth / 2 - Piece.getWidth(piece, Piece.Up) / 2, 0),
  );
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

  let next = state => {
    let (p, orientation, (x, y)) = state.currentPiece;
    let newPoints = Piece.getPoints(p, orientation, (x, y + 1));
    let isColliding =
      List.filter(
        point =>
          List.exists(
            collisionPoint => collisionPoint == point,
            state.collisionPoints,
          ),
        newPoints,
      )
      != [];

    if (isColliding) {
      {
        ...state,
        currentPiece: makeRandomPiece(),
        pastPieces: List.append(state.pastPieces, [state.currentPiece]),
        collisionPoints:
          List.append(
            state.collisionPoints,
            Piece.getPoints(p, orientation, (x, y)),
          ),
      };
    } else {
      {...state, currentPiece: stepPiece(state)};
    };
  };
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
    currentPiece: makeRandomPiece(),
    pastPieces: [],
    collisionPoints: [
      (0, 17),
      (1, 17),
      (2, 17),
      (3, 17),
      (4, 17),
      (5, 17),
      (6, 17),
      (7, 17),
      (8, 17),
      (9, 17),
      (10, 17),
    ],
    board: Board.make(),
  };
};

let draw = (state, env) => {
  Draw.background(grayBackground, env);
  Board.draw(state, env);

  let newStepTimer = state.stepTimer +. Env.deltaTime(env);
  let isNextStep = newStepTimer > stepInterval;
  let stepTimer = isNextStep ? 0.0 : newStepTimer;

  let newState = isNextStep ? Board.next(state) : state;
  let (piece, orientation, point) = newState.currentPiece;
  Piece.draw(~piece, ~orientation, ~point, env);
  List.iter(
    ((piece, orientation, point)) =>
      Piece.draw(~piece, ~orientation, ~point, env),
    newState.pastPieces,
  );

  {...newState, stepTimer};
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

let rotate = orientation =>
  switch (orientation) {
  | Piece.Up => Piece.Right
  | Piece.Right => Piece.Down
  | Piece.Down => Piece.Left
  | Piece.Left => Piece.Up
  };

let keyPressed = (state, env) =>
  switch (Env.keyCode(env)) {
  | Up =>
    let (p, orientation, (x, y)) = state.currentPiece;
    {...state, currentPiece: (p, rotate(orientation), (x, y))};
  | Down => state
  | Left => {...state, currentPiece: moveLeft(state.currentPiece)}
  | Right => {...state, currentPiece: moveRight(state.currentPiece)}
  | _ => state
  };

Reprocessing.run(~setup, ~draw, ~keyPressed, ());

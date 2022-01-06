let
  func = import ./funcutils.nix;
  list = import ./list.nix;
in
rec {
  just = x: { isJust = true; fromJust = x; };
  isJust = { isJust, fromJust}: isJust;

  nothing = { isJust = false; fromJust = null; };
  isNothing = { isJust, fromJust}: !isJust;

  fmap = f: { isJust, fromJust }:
    if isJust
    then { isJust = true; fromJust = f fromJust; }
    else { isJust = false; fromJust = null; };

  maybeIf = shouldJust: val:
    if shouldJust then just val else nothing;

  pure = just;
  apply = f: optionalVal:
    if isNothing f
    then nothing
    else
      if isNothing optionalVal
      then nothing
      else just (f.fromJust optionalVal.fromJust);

  return = just;

  join = optionalOptionalVal:
    if isJust optionalOptionalVal
    then optionalOptionalVal.fromJust
    else nothing;

  bind = optionalVal: f:
    if isNothing optionalVal
    then nothing
    else f optionalVal.fromJust;

  fromMaybe = default: optionalValue:
    if isJust optionalValue
    then optionalValue.fromJust
    else default;

  maybe = default: f: optionalVal:
    fromMaybe default (fmap f optionalVal);

  listToMaybe = lst:
    if builtins.length lst == 0 then nothing else just (builtins.head lst);

  maybeToList = optionalVal:
    if isJust optionalVal then [optionalVal.fromJust] else [];

  consMaybe = optionalVal: lst:
    (maybeToList optionalVal) ++ lst;

  catMaybes = optionalVals:
    list.reverse (builtins.foldl' (func.flip consMaybe) [] optionalVals);

  alternative = a: b:
    if isJust a then a else b;
}

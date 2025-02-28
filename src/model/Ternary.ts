export enum Ternary {
  True = 'True',
  False = 'False',
  Unknown = 'Unknown'
}

export function ternary2string(value: Ternary): string {
  switch (value) {
    case Ternary.True:
      return "true";
    case Ternary.False:
      return "false";
    case Ternary.Unknown:
      return "undefined";
  }
}

export function ternary2bool(value: Ternary): boolean | undefined {
  switch (value) {
    case Ternary.True:
      return true;
    case Ternary.False:
      return false;
    case Ternary.Unknown:
      return undefined;
  }
}

export function ternaryFromString(value: string): Ternary {
  switch (value) {
    case "true":
      return Ternary.True;
    case "false":
      return Ternary.False;
    default:
      return Ternary.Unknown;
  }
}

export function not3(value: Ternary): Ternary {
  switch (value) {
    case Ternary.True:
      return Ternary.False;
    case Ternary.False:
      return Ternary.True;
    case Ternary.Unknown:
      return Ternary.Unknown;
  }
}

@0xe81d238ec50a0daa;

interface Store {
  struct Response {
    union {
      ok @0 :Text;
      notFound @1 :Void;
    }
  }

  get @0 (path :List(Text)) -> Response;
}

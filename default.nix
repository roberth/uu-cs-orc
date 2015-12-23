let env = import ./env;
in {
   timekeeper = import ./timekeeper { inherit env; };
   proposal-presentation = import ./doc/proposal { inherit env; };
}

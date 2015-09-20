
connector Pin
  Real v;
  flow Real i;
end Pin;

model Resistor
  Pin p, n;
algorithm
  assert(cardinality(p) > 0 and cardinality(n) > 0, "Connectors p and n of Resistor must be connected");
end Resistor;


block comment2
  input Real in0;
  input Real in1;
  input Real x;
  input Boolean positiveVelocity;
protected
  Real points[:];
  Real values[:]; // comment
  Real x0;
  Blub b(text = "hello /* comment */ hello // hello");
  Integer m;
algorithm

  when true then
    if x>x0 then
      m:=size(points, 1);
    elseif x<x0 then

      while true loop
	m:=m+1;

      end while;

    end if;

    x0:=x;
  end when;

end comment2;


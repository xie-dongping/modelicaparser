block spatialDistribution
  input Real in0;
  input Real in1;
  input Real x;
  input Boolean positiveVelocity;
  parameter Real initialPoints(each min=0, each max=1) = {0.0, 1.0};
  parameter Real initialValues[:] = {0.0, 0.0};
  output Real out0;
  output Real out1;
protected
  Real points[:];
  Real values[:];
  Real x0;
  Integer m;
algorithm
  if positiveVelocity then
    out1:=interpolate(points, values, 1-(x-x0));
    out0:=values[1];
  else
    out0:=interpolate(points, values, (x-x0));
    out1:=values[end];
  end if;

  when acceptedStep then
    if x>x0 then
      m:=size(points, 1);
      while (if m>0 then points[m]+(x-x0)>=1 else false) loop
		 m:=m-1;
      end while;

      values:=cat(1, {in0}, values[1:m], {interpolate(points, values,1-(x-x0))});
      points:=cat(1, {0}, points[1:m] .+ (x1-x0), {1} );
    elseif x<x0 then
      m:=1;
      while (if m<size(points,1) then points[m]+(x-x0)<=0 else false) loop
		 m:=m+1;
      end while;
      values:=cat(1, {interpolate(points, values, 0-(x-x0))},values[m:end],{in1});
      points:=cat(1, {0}, points[m:end] .+ (x1-x0), {1});
    end if;
    x0:=x;
  end when;
  initial algorithm
  x0:=x;
  points:=initialPoints;
  values:=initialValues;
end spatialDistribution;


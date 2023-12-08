open Complex

let pf, sf = Printf.printf, Scanf.sscanf

let print_complex z =
  let y = z.im
  and x = z.re in

  if x = 0.0 then
    let y' = if y = 0.0 then 0.0 else y in
    pf "%.0fj" y';
  else
    if y > 0.0 then pf "(%.0f+%.0fj)" x y
    else if y < 0.0 then pf "(%.0f-%.0fj)" x (-.y)
    else pf "(%.0f+0j)" x

let closest (n : float) : float =
  let c = ceil n
  and f = floor n in
  let d = abs_float (n -. c) in
  if d <= 0.5 then c else f

let rec gcd z1 z2 =
  let z = div z1 z2 in
  let y = z.im
  and x = z.re in

  let cy = closest y
  and cx = closest x in

  let q = { re = cx; im = cy} in
  let r = add z1 (mul (neg z2) q) in

  print_complex z1; pf " = ";
  print_complex z2; pf " * ";
  print_complex q;  pf " + ";
  print_complex r;  pf "\n";

  if r = Complex.zero then z2 else gcd z2 r


let () =
  let parse_complex x y = { re = x; im = y } in

  let z1 = sf (read_line()) "%f %f" parse_complex in
  let z2 = sf (read_line()) "%f %f" parse_complex in

  let g = gcd z1 z2 in

  pf "GCD("; print_complex z1;
  pf ", ";   print_complex z2;
  pf ") = "; print_complex g;

TAGS = { "<<" => ">>", "[[" => "]]" }

def apply_case(s, cur_case)
  cur_case ? s.send(cur_case) : s
end

def solve(s)
  stack = [] # list of [stylem, opening_tag]
  skip = 0
  cur_case = nil
  res = ""
  buf = ""

  s.each_char.with_index do |c, idx|
    next if idx < skip

    slice = s[idx..]
    TAGS.each do |otag, ctag|
      if slice.start_with?(ctag)
        next unless stack[-1] && otag == stack[-1][1]

        pre_case, opening = stack.pop
        res += apply_case(buf, cur_case)
        cur_case = pre_case
        buf, skip = "", idx + 2
      elsif slice.start_with?(otag)
        next unless slice[/#{Regexp.quote(ctag)}/]
        # (NEW) Did we open the same tag before?
        next if stack[-1] && otag == stack[-1][1]

        res += apply_case(buf, cur_case)
        stack << [cur_case, otag]
        cur_case = otag == "<<" ? :upcase : :downcase
        buf, skip = "", idx + 2
      end
    end

    buf += c if idx >= skip
  end

  res += apply_case(buf, cur_case) if buf.size > 0

  res
end

gets.to_i.times do
  line = gets.chomp
  puts solve(line).gsub(/  +/, " ").strip
end

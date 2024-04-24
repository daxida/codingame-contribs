def get_lis(ary)
  n = ary.size
  dp = Array.new(n, 1)
  prev = Array.new(n, -1)
  (0...n).each do |i|
    (0...i).each do |j|
      if (ary[j] < ary[i] && dp[i] < dp[j] + 1)
        dp[i] = dp[j] + 1
        prev[i] = j
      end
    end
  end

  max_sz = dp.max
  solutions = (0...dp.size).select { |idx| dp[idx] == max_sz }

  ans = []
  pos = solutions[0]
  until pos == -1
    ans << ary[pos]
    pos = prev[pos]
  end

  ans.reverse
end

def patch_order(chapters)
  to_patch = chapters - get_lis(chapters)
  to_patch.sort
end

def get_requests(mapping, order)
  chapters_ids = mapping.keys
  fix_members = [0] + (chapters_ids - order)

  requests = []
  order.each_with_index do |chapter_id, idx|
    first_bigger_fix_member_idx = (0...fix_members.size).find { |nidx| fix_members[nidx] > chapter_id }
    first_bigger_fix_member_idx ||= fix_members.size
    first_bigger_fix_member_idx -= 1

    should_go_after_this = fix_members[first_bigger_fix_member_idx]
    fix_members.insert(first_bigger_fix_member_idx + 1, chapter_id)
    should_go = (chapters_ids.index(should_go_after_this) || -1) + 1

    # And simulate to double check
    chapter_idx = chapters_ids.index(chapter_id)
    chapters_ids.delete_at(chapter_idx)
    offset = should_go > chapter_idx ? 1 : 0
    chapters_ids.insert(should_go - offset, chapter_id)
    requests << [mapping[chapter_id], should_go - offset + 1] * " "
  end

  raise "NotSortedError" unless chapters_ids == chapters_ids.sort

  requests
end

sz = gets.to_i
chapters = sz.times.map { gets.chomp }

mapping = chapters.map do |chapter|
  [chapter.match(/\d+/)[0].to_i, chapter]
end.to_h
chapters_ids = mapping.keys
order = patch_order(chapters_ids)

puts order.size, get_requests(mapping, order)

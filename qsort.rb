def qsort(list)
  return [] if list.empty?
  pivot = list.shift
  smaller = []
  larger = []

  list.each do |item|
    smaller << item if item < pivot
    larger << item if item >= pivot
  end
  return qsort(smaller) + [pivot] + qsort(larger)
end

include ErlPort::ErlTerm

class T
  def self.[](*data)
    Tuple.new(data)
  end
end

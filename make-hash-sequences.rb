require 'digest'

Modulus = ARGV[0].to_i
STDIN.each_line {|line| puts Digest::MD5.hexdigest(line.chomp).to_i(16) % Modulus }

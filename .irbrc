require 'pp'
require 'ap'

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:SAVE_HISTORY] = 10_000

def load_irbrc(path)
  return if (path == ENV["HOME"]) || (path == '/')
  load_irbrc(File.dirname path)
  irbrc = File.join(path, ".irbrc")
  load irbrc if File.exists?(irbrc)
end

load_irbrc Dir.pwd

module IRB
  class Irb
    def output_value
      ap @context.last_value
    end
  end
end

#!/usr/bin/env ruby

require 'erb'
require 'json'

def rules(*args)
  data = []
  complex_modifiers = {}

  Dir["#{ENV['KARABINER_COMPLEX_MODIFICATIONS_DIR']}/*.json"].each do |json_file|
    filename = File.basename(json_file, '.json')
    complex_modifiers[filename] = JSON.parse(File.read(json_file))['rules']
  end

  args.each do |complex_modifier|
    complex_modifier, *rules_indices = complex_modifier
    ruleset = complex_modifiers[complex_modifier] || []
    data += rules_indices.empty? ? ruleset : ruleset.values_at(*rules_indices)
  end

  JSON.generate(data.compact)
end

template = ERB.new $stdin.read
puts JSON.pretty_generate(JSON.parse(template.result))

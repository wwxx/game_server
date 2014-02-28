class ApiParser
  COMMON_TYPES = ['short', 'integer', 'float', 'string']


  def generate_encode(pairs)
    list = []
    pairs.each do |key, data_type|
      case data_type
      when 'string'
        list << "utils_protocol:encode_string(#{key.camelcase})"
      when 'integer'
        list << "utils_protocol:encode_integer(#{key.camelcase})"
      when 'float'
        list << "utils_protocol:encode_float(#{key.camelcase})"
      when 'short'
        list << "utils_protocol:encode_short(#{key.camelcase})"
      else
        if response.keys.include?(data_type)
          list << "encode(#{key}, #{key.camelcase})"
          decode_list << "{#{key.camelcase}, Bin#{key_no+1}} = response_decoder:decode(Bin#{key_no})"
        elsif data_type.index("array-") == 0
          _type, element_name = data_type.split('-')
          list << "utils_protocol:encode_array(#{key.camelcase}, fun(Item) -> response_encoder:encode(#{element_name}, Item) end)"
          decode_list << "{#{key.camelcase}, Bin#{key_no+1}} = utils_protocol:decode_array(Bin#{key_no}, fun(Data) -> response_decoder:decode(Data) end)"
        else
          raise "Wrong Data Type: #{data_type}"
        end
      end
    end
  end
end

package com.g2forge.reassert.maven.model.convert;

import java.io.IOException;
import java.util.Collection;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.BeanProperty;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.deser.std.CollectionDeserializer;

public class XmlWhitespaceCollectionDeserialiser extends CollectionDeserializer {
	private static final long serialVersionUID = 6909524481453384474L;

	public XmlWhitespaceCollectionDeserialiser(CollectionDeserializer deserializer) {
		super(deserializer);
	}

	@Override
	public CollectionDeserializer createContextual(DeserializationContext context, BeanProperty property) throws JsonMappingException {
		return new XmlWhitespaceCollectionDeserialiser(super.createContextual(context, property));
	}

	@Override
	@SuppressWarnings("unchecked")
	public Collection<Object> deserialize(JsonParser parser, DeserializationContext context) throws IOException, JsonProcessingException {
		if ((parser.getCurrentToken() == JsonToken.VALUE_STRING) && parser.getText().matches("^\\s+$")) return (Collection<Object>) _valueInstantiator.createUsingDefault(context);
		return super.deserialize(parser, context);
	}
}
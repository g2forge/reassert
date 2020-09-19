package com.g2forge.reassert.reassert.convert.contract;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.deser.ResolvableDeserializer;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.databind.jsontype.TypeDeserializer;
import com.g2forge.alexandria.java.function.IThrowSupplier;
import com.g2forge.reassert.core.model.IVertex;

import lombok.AccessLevel;
import lombok.Getter;

@Getter(AccessLevel.PROTECTED)
public class ContractDeserializer extends StdDeserializer<IVertex> implements ResolvableDeserializer {
	private static final long serialVersionUID = -6856117256176037393L;

	protected final JsonDeserializer<?> deserializer;

	protected final ContractParser parser;

	protected ContractDeserializer(JsonDeserializer<?> deserializer, ContractParser parser) {
		super(IVertex.class);
		this.deserializer = deserializer;
		this.parser = parser;
	}

	@Override
	public IVertex deserialize(JsonParser parser, DeserializationContext context) throws IOException, JsonProcessingException {
		return (IVertex) deserialize(parser, () -> getDeserializer().deserialize(parser, context));
	}

	protected Object deserialize(JsonParser parser, IThrowSupplier<?, IOException> supplier) throws IOException {
		final JsonToken token = parser.currentToken();
		if (token == JsonToken.VALUE_STRING) {
			final String text = parser.getText().trim();
			return getParser().parse(text);
		}
		return supplier.get();
	}

	@Override
	public Object deserializeWithType(JsonParser parser, DeserializationContext context, TypeDeserializer typeDeserializer) throws IOException {
		return deserialize(parser, () -> super.deserializeWithType(parser, context, typeDeserializer));
	}

	@Override
	public void resolve(DeserializationContext context) throws JsonMappingException {
		final JsonDeserializer<?> deserializer = getDeserializer();
		if (deserializer instanceof ResolvableDeserializer) ((ResolvableDeserializer) deserializer).resolve(context);
	}
}
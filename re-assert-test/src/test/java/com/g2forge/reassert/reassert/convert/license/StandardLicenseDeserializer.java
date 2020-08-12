package com.g2forge.reassert.reassert.convert.license;

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
import com.g2forge.reassert.license.StandardLicense;

public class StandardLicenseDeserializer extends StdDeserializer<IVertex> implements ResolvableDeserializer {
	private static final long serialVersionUID = -6856117256176037393L;

	protected final JsonDeserializer<?> deserializer;

	protected StandardLicenseDeserializer(JsonDeserializer<?> deserializer) {
		super(IVertex.class);
		this.deserializer = deserializer;
	}

	@Override
	public IVertex deserialize(JsonParser parser, DeserializationContext context) throws IOException, JsonProcessingException {
		return (IVertex) deserialize(parser, () -> deserializer.deserialize(parser, context));
	}

	protected Object deserialize(JsonParser parser, IThrowSupplier<?, IOException> supplier) throws IOException {
		final JsonToken token = parser.currentToken();
		if (token == JsonToken.VALUE_STRING) return fromString(parser);
		return supplier.get();
	}

	@Override
	public Object deserializeWithType(JsonParser parser, DeserializationContext context, TypeDeserializer typeDeserializer) throws IOException {
		return deserialize(parser, () -> super.deserializeWithType(parser, context, typeDeserializer));
	}

	protected StandardLicense fromString(JsonParser parser) throws IOException {
		return StandardLicense.valueOf(parser.getText().trim());
	}

	@Override
	public void resolve(DeserializationContext context) throws JsonMappingException {
		if (deserializer instanceof ResolvableDeserializer) ((ResolvableDeserializer) deserializer).resolve(context);
	}
}
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
import com.g2forge.alexandria.java.core.error.HError;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IThrowSupplier;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.standard.model.contract.license.StandardLicense;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsage;

public class StandardContractDeserializer extends StdDeserializer<IVertex> implements ResolvableDeserializer {
	private static final long serialVersionUID = -6856117256176037393L;

	protected final JsonDeserializer<?> deserializer;

	protected StandardContractDeserializer(JsonDeserializer<?> deserializer) {
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

	protected IVertex fromString(JsonParser parser) throws IOException {
		final String text = parser.getText().trim();
		return HError.apply((IFunction1<String, IVertex> valueOf) -> valueOf.apply(text), String.format("Could not parse \"%1$s\" as a standard contract", text), StandardLicense::valueOf, StandardUsage::valueOf);
	}

	@Override
	public void resolve(DeserializationContext context) throws JsonMappingException {
		if (deserializer instanceof ResolvableDeserializer) ((ResolvableDeserializer) deserializer).resolve(context);
	}
}
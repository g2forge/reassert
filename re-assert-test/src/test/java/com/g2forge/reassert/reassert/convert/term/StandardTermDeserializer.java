package com.g2forge.reassert.reassert.convert.term;

import java.io.IOException;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.deser.ResolvableDeserializer;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.databind.jsontype.TypeDeserializer;
import com.g2forge.alexandria.java.core.helpers.HStream;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.contract.term.StandardLicenseTerm;
import com.g2forge.reassert.contract.term.StandardUsageTerm;
import com.g2forge.reassert.core.model.contract.ITerm;

import lombok.AccessLevel;
import lombok.Getter;

public class StandardTermDeserializer extends StdDeserializer<ITerm> implements ResolvableDeserializer {
	private static final long serialVersionUID = 3742304528846339144L;

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final Map<String, ITerm> terms = computeTerms();

	protected static Map<String, ITerm> computeTerms() {
		return HStream.concat(Stream.of(StandardLicenseTerm.values()), Stream.of(StandardUsageTerm.values())).collect(Collectors.toMap(Enum::name, IFunction1.Identity.create()));
	}

	protected final JsonDeserializer<?> deserializer;

	protected StandardTermDeserializer(JsonDeserializer<?> deserializer) {
		super(ITerm.class);
		this.deserializer = deserializer;
	}

	@Override
	public ITerm deserialize(JsonParser parser, DeserializationContext context) throws IOException, JsonProcessingException {
		final JsonToken token = parser.currentToken();
		if (token == JsonToken.VALUE_STRING) return fromString(parser);
		return (ITerm) deserializer.deserialize(parser, context);
	}

	@Override
	public Object deserializeWithType(JsonParser parser, DeserializationContext context, TypeDeserializer typeDeserializer) throws IOException {
		final JsonToken token = parser.currentToken();
		if (token == JsonToken.VALUE_STRING) return fromString(parser);
		return super.deserializeWithType(parser, context, typeDeserializer);
	}

	protected ITerm fromString(JsonParser parser) throws IOException {
		final String string = parser.getText().trim();
		final ITerm term = getTerms().get(string);
		if (term == null) throw new IllegalArgumentException(String.format("Unrecognized as standard term: \"%1$s\"!", string));
		return term;
	}

	@Override
	public void resolve(DeserializationContext context) throws JsonMappingException {
		if (deserializer instanceof ResolvableDeserializer) ((ResolvableDeserializer) deserializer).resolve(context);
	}
}
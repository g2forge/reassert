package com.g2forge.reassert.list.convert.simpleedge;

import java.io.IOException;
import java.util.Map;
import java.util.stream.Collectors;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.deser.ResolvableDeserializer;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.databind.jsontype.TypeDeserializer;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.ISupplier;
import com.g2forge.reassert.core.model.Copy;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.core.model.artifact.Invokes;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.coordinates.Coordinates;
import com.g2forge.reassert.core.model.file.Contains;
import com.g2forge.reassert.core.model.file.Describes;
import com.g2forge.reassert.core.model.file.Parsed;
import com.g2forge.reassert.core.model.work.WorkLicense;
import com.g2forge.reassert.core.model.work.WorkMember;

public class SimpleEdgeDeserializer extends StdDeserializer<IEdge> implements ResolvableDeserializer {
	private static final long serialVersionUID = 3742304528846339144L;

	protected static final Map<String, ISupplier<? extends IEdge>> suppliers = HCollection.<Class<? extends IEdge>>asList(Contains.class, Coordinates.class, Copy.class, Describes.class, Inherits.class, Invokes.class, Notice.class, Parsed.class, WorkMember.class, WorkLicense.class).stream().collect(Collectors.toMap(c -> c.getSimpleName().toLowerCase(), c -> {
		return ISupplier.create(() -> {
			try {
				return c.newInstance();
			} catch (InstantiationException | IllegalAccessException e) {
				throw new RuntimeException(e);
			}
		});
	}));

	protected final JsonDeserializer<?> deserializer;

	protected SimpleEdgeDeserializer(JsonDeserializer<?> deserializer) {
		super(IEdge.class);
		this.deserializer = deserializer;
	}

	@Override
	public IEdge deserialize(JsonParser parser, DeserializationContext context) throws IOException, JsonProcessingException {
		final JsonToken token = parser.currentToken();
		if (token == JsonToken.VALUE_STRING) return fromString(parser);
		return (IEdge) deserializer.deserialize(parser, context);
	}

	@Override
	public Object deserializeWithType(JsonParser parser, DeserializationContext context, TypeDeserializer typeDeserializer) throws IOException {
		final JsonToken token = parser.currentToken();
		if (token == JsonToken.VALUE_STRING) return fromString(parser);
		return super.deserializeWithType(parser, context, typeDeserializer);
	}

	protected IEdge fromString(JsonParser parser) throws IOException {
		final String string = parser.getText().trim().toLowerCase();
		final ISupplier<? extends IEdge> supplier = suppliers.get(string);
		if (supplier == null) throw new IllegalArgumentException(String.format("Cannot construct edges of type \"%1$s\"!", string));
		return supplier.get();
	}

	@Override
	public void resolve(DeserializationContext context) throws JsonMappingException {
		if (deserializer instanceof ResolvableDeserializer) ((ResolvableDeserializer) deserializer).resolve(context);
	}
}
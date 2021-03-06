package com.g2forge.reassert.reassert.convert.contract;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.jsontype.TypeSerializer;
import com.fasterxml.jackson.databind.ser.ResolvableSerializer;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IThrowConsumer1;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.contract.IContractApplied;

import lombok.AccessLevel;
import lombok.Getter;

@Getter(AccessLevel.PROTECTED)
public class ContractSerializer extends StdSerializer<IVertex> implements ResolvableSerializer {
	private static final long serialVersionUID = -6423425543439855466L;

	protected final JsonSerializer<?> serializer;

	protected final IFunction1<? super Object, ? extends IDescription> describer;

	protected final ContractParser parser;

	protected ContractSerializer(JsonSerializer<?> serializer, IFunction1<? super Object, ? extends IDescription> describer, ContractParser parser) {
		super(IVertex.class);
		this.serializer = serializer;
		this.describer = describer;
		this.parser = parser;
	}

	@Override
	public void resolve(SerializerProvider provider) throws JsonMappingException {
		final JsonSerializer<?> serializer = getSerializer();
		if (serializer instanceof ResolvableSerializer) ((ResolvableSerializer) serializer).resolve(provider);
	}

	protected void serialize(IVertex value, JsonGenerator generator, IThrowConsumer1<? super JsonSerializer<? super IVertex>, IOException> fallback) throws IOException {
		final String identifier = getDescriber().apply(value).getIdentifier();

		IContractApplied parsed = null;
		try {
			parsed = getParser().parse(identifier);
		} catch (Throwable throwable) {}

		if ((parsed != null) && value.equals(parsed)) {
			generator.writeString(identifier);
		} else {
			@SuppressWarnings("unchecked")
			final JsonSerializer<? super IVertex> serializer = (JsonSerializer<? super IVertex>) getSerializer();
			fallback.accept(serializer);
		}
	}

	@Override
	public void serialize(IVertex value, JsonGenerator generator, SerializerProvider provider) throws IOException {
		serialize(value, generator, serializer -> generator.writeString(getDescriber().apply(value).getIdentifier()));
	}

	public void serializeWithType(IVertex value, JsonGenerator generator, SerializerProvider provider, TypeSerializer typeSerializer) throws IOException {
		serialize(value, generator, serializer -> serializer.serializeWithType(value, generator, provider, typeSerializer));
	}
}
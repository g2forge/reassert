package com.g2forge.reassert.reassert.convert.work;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.jsontype.TypeSerializer;
import com.fasterxml.jackson.databind.ser.ResolvableSerializer;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.g2forge.alexandria.java.function.IThrowConsumer1;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.RuleWorkType;
import com.g2forge.reassert.core.model.work.IWorkType;

import lombok.AccessLevel;
import lombok.Getter;

@Getter(AccessLevel.PROTECTED)
public class WorkTypeSerializer extends StdSerializer<IWorkType> implements ResolvableSerializer {
	private static final long serialVersionUID = 6716125188445626724L;

	protected final JsonSerializer<?> serializer;

	protected WorkTypeSerializer(JsonSerializer<?> serializer) {
		super(IWorkType.class);
		this.serializer = serializer;
	}

	@Override
	public void resolve(SerializerProvider provider) throws JsonMappingException {
		final JsonSerializer<?> serializer = getSerializer();
		if (serializer instanceof ResolvableSerializer) ((ResolvableSerializer) serializer).resolve(provider);
	}

	protected void serialize(IWorkType value, JsonGenerator generator, IThrowConsumer1<? super JsonSerializer<? super IWorkType>, IOException> fallback) throws IOException {
		if (value instanceof RuleWorkType) {
			generator.writeObject(((RuleWorkType) value).getRule());
		} else {
			@SuppressWarnings("unchecked")
			final JsonSerializer<? super IWorkType> serializer = (JsonSerializer<? super IWorkType>) getSerializer();
			fallback.accept(serializer);
		}
	}

	@Override
	public void serialize(IWorkType value, JsonGenerator generator, SerializerProvider provider) throws IOException {
		serialize(value, generator, serializer -> serializer.serialize(value, generator, provider));
	}

	public void serializeWithType(IWorkType value, JsonGenerator generator, SerializerProvider provider, TypeSerializer typeSerializer) throws IOException {
		serialize(value, generator, serializer -> serializer.serializeWithType(value, generator, provider, typeSerializer));
	}
}
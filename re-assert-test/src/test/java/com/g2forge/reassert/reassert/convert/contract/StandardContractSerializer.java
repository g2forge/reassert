package com.g2forge.reassert.reassert.convert.contract;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.jsontype.TypeSerializer;
import com.fasterxml.jackson.databind.ser.ResolvableSerializer;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.standard.model.contract.license.StandardLicense;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsage;

public class StandardContractSerializer extends StdSerializer<IVertex> implements ResolvableSerializer {
	private static final long serialVersionUID = -6423425543439855466L;

	protected final JsonSerializer<?> serializer;

	protected StandardContractSerializer(JsonSerializer<?> serializer) {
		super(IVertex.class);
		this.serializer = serializer;
	}

	@Override
	public void resolve(SerializerProvider provider) throws JsonMappingException {
		if (serializer instanceof ResolvableSerializer) ((ResolvableSerializer) serializer).resolve(provider);
	}

	@Override
	public void serialize(IVertex value, JsonGenerator generator, SerializerProvider provider) throws IOException {
		if (value instanceof StandardLicense) generator.writeString(((StandardLicense) value).name());
		else if (value instanceof StandardUsage) generator.writeString(((StandardUsage) value).name());
		else {
			@SuppressWarnings("unchecked")
			final JsonSerializer<? super IVertex> cast = (JsonSerializer<? super IVertex>) serializer;
			cast.serialize(value, generator, provider);
		}
	}

	public void serializeWithType(IVertex value, JsonGenerator generator, SerializerProvider provider, TypeSerializer typeSerializer) throws IOException {
		if ((value instanceof StandardLicense) || (value instanceof StandardUsage)) serialize(value, generator, provider);
		else {
			@SuppressWarnings("unchecked")
			final JsonSerializer<? super IVertex> cast = (JsonSerializer<? super IVertex>) serializer;
			cast.serializeWithType(value, generator, provider, typeSerializer);
		}
	}
}
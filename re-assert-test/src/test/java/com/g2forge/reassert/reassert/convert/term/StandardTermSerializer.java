package com.g2forge.reassert.reassert.convert.term;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.jsontype.TypeSerializer;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.g2forge.reassert.core.model.contract.terms.ITerm;

public class StandardTermSerializer extends StdSerializer<ITerm> {
	private static final long serialVersionUID = 3043637284673288902L;

	protected StandardTermSerializer() {
		super(ITerm.class);
	}

	@Override
	public void serialize(ITerm value, JsonGenerator generator, SerializerProvider provider) throws IOException {
		@SuppressWarnings("unchecked")
		final Enum<? extends ITerm> cast = (Enum<? extends ITerm>) value;
		generator.writeString(cast.name());
	}

	public void serializeWithType(ITerm value, JsonGenerator generator, SerializerProvider provider, TypeSerializer typeSerializer) throws IOException {
		serialize(value, generator, provider);
	}
}
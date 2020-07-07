package com.g2forge.reassert.list.convert.simpleedge;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.jsontype.TypeSerializer;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.g2forge.reassert.core.model.IEdge;

public class SimpleEdgeSerializer extends StdSerializer<IEdge> {
	private static final long serialVersionUID = 3043637284673288902L;

	protected SimpleEdgeSerializer() {
		super(IEdge.class);
	}

	@Override
	public void serialize(IEdge value, JsonGenerator generator, SerializerProvider provider) throws IOException {
		generator.writeString(value.getClass().getSimpleName().toLowerCase());
	}

	public void serializeWithType(IEdge value, JsonGenerator generator, SerializerProvider provider, TypeSerializer typeSerializer) throws IOException {
		serialize(value, generator, provider);
	}
}
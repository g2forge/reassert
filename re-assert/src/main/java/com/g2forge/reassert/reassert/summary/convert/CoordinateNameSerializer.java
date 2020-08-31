package com.g2forge.reassert.reassert.summary.convert;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.Getter;

@Getter
public class CoordinateNameSerializer extends StdSerializer<ICoordinates> {
	private static final long serialVersionUID = 3476943857324528170L;

	protected final IFunction1<? super ICoordinates, ? extends IDescription> describer;

	protected CoordinateNameSerializer(IFunction1<? super ICoordinates, ? extends IDescription> describer) {
		super(ICoordinates.class);
		this.describer = describer;
	}

	@Override
	public void serialize(ICoordinates value, JsonGenerator generator, SerializerProvider provider) throws IOException {
		generator.getCodec().writeValue(generator, getDescriber().apply(value).getName());
	}
}
package com.g2forge.reassert.reassert.summary.convert;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.contract.IContract;

import lombok.Getter;

@Getter
public class ContractSerializer extends StdSerializer<IContract> {
	private static final long serialVersionUID = 437022414208677193L;

	protected final IFunction1<IVertex, IDescription> vertexDescriber;

	protected ContractSerializer(IFunction1<IVertex, IDescription> vertexDescriber) {
		super(IContract.class);
		this.vertexDescriber = vertexDescriber;
	}

	@Override
	public void serialize(IContract value, JsonGenerator generator, SerializerProvider provider) throws IOException {
		generator.getCodec().writeValue(generator, getVertexDescriber().apply(value).getName());
	}
}
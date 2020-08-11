package com.g2forge.reassert.reassert.summary.convert;

import java.io.IOException;
import java.util.stream.Collectors;

import org.jgrapht.GraphPath;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;

import lombok.Getter;

@Getter
public class PathSerializer extends StdSerializer<GraphPath<IVertex, IEdge>> {
	private static final long serialVersionUID = -64732562539487524L;

	protected final IFunction1<? super IVertex, ? extends IDescription> vertexDescriber;

	protected PathSerializer(IFunction1<? super IVertex, ? extends IDescription> vertexDescriber) {
		super(GraphPath.class, false);
		this.vertexDescriber = vertexDescriber;
	}

	@Override
	public void serialize(GraphPath<IVertex, IEdge> value, JsonGenerator generator, SerializerProvider provider) throws IOException {
		final String string = value.getVertexList().stream().map(getVertexDescriber()).map(IDescription::getName).collect(Collectors.joining(" -> "));
		generator.getCodec().writeValue(generator, string);
	}
}
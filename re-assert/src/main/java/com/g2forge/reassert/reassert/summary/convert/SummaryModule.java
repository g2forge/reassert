package com.g2forge.reassert.reassert.summary.convert;

import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializationConfig;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.BeanSerializerModifier;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class SummaryModule extends SimpleModule {
	private static final long serialVersionUID = 840399436131646940L;

	protected final IFunction1<IVertex, IDescription> vertexDescriber;

	@Override
	public void setupModule(SetupContext context) {
		super.setupModule(context);
		
		context.addBeanSerializerModifier(new BeanSerializerModifier() {
			@Override
			public JsonSerializer<?> modifySerializer(SerializationConfig config, BeanDescription description, JsonSerializer<?> serializer) {
				if (ICoordinates.class.isAssignableFrom(description.getBeanClass())) return new CoordinateNameSerializer(getVertexDescriber());
				return serializer;
			}
		});
	}
}
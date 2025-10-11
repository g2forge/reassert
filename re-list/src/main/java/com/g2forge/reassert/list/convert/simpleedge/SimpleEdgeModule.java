package com.g2forge.reassert.list.convert.simpleedge;

import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.DeserializationConfig;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializationConfig;
import com.fasterxml.jackson.databind.deser.BeanDeserializerModifier;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.BeanSerializerModifier;
import com.g2forge.reassert.core.model.IEdge;

public class SimpleEdgeModule extends SimpleModule {
	private static final long serialVersionUID = 8212478409528352657L;

	@Override
	public void setupModule(SetupContext context) {
		super.setupModule(context);
		context.addBeanDeserializerModifier(new BeanDeserializerModifier() {
			private static final long serialVersionUID = -7441441779625262166L;

			public JsonDeserializer<?> modifyDeserializer(DeserializationConfig config, BeanDescription description, JsonDeserializer<?> deserializer) {
				if (IEdge.class.isAssignableFrom(description.getBeanClass())) return new SimpleEdgeDeserializer(deserializer);
				return deserializer;
			}
		});
		context.addBeanSerializerModifier(new BeanSerializerModifier() {
			private static final long serialVersionUID = -5870420170827620394L;

			@Override
			public JsonSerializer<?> modifySerializer(SerializationConfig config, BeanDescription description, JsonSerializer<?> serializer) {
				if (IEdge.class.isAssignableFrom(description.getBeanClass()) && description.findProperties().isEmpty()) return new SimpleEdgeSerializer();
				return serializer;
			}
		});
	}
}
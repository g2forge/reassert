package com.g2forge.reassert.maven.model.convert;

import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.DeserializationConfig;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.deser.BeanDeserializerModifier;
import com.fasterxml.jackson.databind.deser.std.CollectionDeserializer;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.type.CollectionType;
import com.g2forge.reassert.maven.model.MavenPackaging;

public class MavenXmlModule extends SimpleModule {
	private static final long serialVersionUID = -2581598831713893816L;

	@Override
	public void setupModule(SetupContext context) {
		addSerializer(new MavenPackagingSerializer());
		addDeserializer(MavenPackaging.class, new MavenPackagingDeserializer());
		super.setupModule(context);
		context.addBeanDeserializerModifier(new BeanDeserializerModifier() {
			@Override
			public JsonDeserializer<?> modifyCollectionDeserializer(DeserializationConfig config, CollectionType type, BeanDescription beanDescription, JsonDeserializer<?> deserializer) {
				if (deserializer instanceof CollectionDeserializer) return new XmlWhitespaceCollectionDeserialiser((CollectionDeserializer) deserializer);

				return super.modifyCollectionDeserializer(config, type, beanDescription, deserializer);
			}
		});
	}

}

package com.g2forge.reassert.reassert.convert.work;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.DeserializationConfig;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializationConfig;
import com.fasterxml.jackson.databind.deser.BeanDeserializerModifier;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.BeanSerializerModifier;
import com.g2forge.reassert.contract.algorithm.work.model.finding.UnknownWorkFinding;
import com.g2forge.reassert.contract.algorithm.work.model.rule.IWorkRule;
import com.g2forge.reassert.core.model.work.IWorkType;

public class WorkModule extends SimpleModule {
	@JsonTypeInfo(use = JsonTypeInfo.Id.MINIMAL_CLASS, include = JsonTypeInfo.As.PROPERTY)
	@JsonInclude(JsonInclude.Include.NON_DEFAULT)
	protected static class PolymorphicWorkRuleMixin {}

	protected static abstract class UnknownWorkTypeFindingMixin {
		@JsonIgnore
		protected Throwable throwable;
	}

	private static final long serialVersionUID = -6056568239544794035L;

	@Override
	public void setupModule(SetupContext context) {
		setMixInAnnotation(UnknownWorkFinding.class, UnknownWorkTypeFindingMixin.class);
		setMixInAnnotation(IWorkRule.class, PolymorphicWorkRuleMixin.class);

		super.setupModule(context);

		context.addBeanDeserializerModifier(new BeanDeserializerModifier() {
			public JsonDeserializer<?> modifyDeserializer(DeserializationConfig config, BeanDescription description, JsonDeserializer<?> deserializer) {
				if (IWorkType.class.isAssignableFrom(description.getBeanClass())) return new WorkTypeDeserializer(deserializer);
				return deserializer;
			}
		});
		context.addBeanSerializerModifier(new BeanSerializerModifier() {
			@Override
			public JsonSerializer<?> modifySerializer(SerializationConfig config, BeanDescription description, JsonSerializer<?> serializer) {
				if (IWorkType.class.isAssignableFrom(description.getBeanClass())) return new WorkTypeSerializer(serializer);
				return serializer;
			}
		});
	}
}
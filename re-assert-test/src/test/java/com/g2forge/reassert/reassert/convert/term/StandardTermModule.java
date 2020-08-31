package com.g2forge.reassert.reassert.convert.term;

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
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

public class StandardTermModule extends SimpleModule {
	@JsonTypeInfo(use = JsonTypeInfo.Id.MINIMAL_CLASS, include = JsonTypeInfo.As.PROPERTY)
	@JsonInclude(JsonInclude.Include.NON_DEFAULT)
	protected static abstract class PolymorphicTermMixin {}

	private static final long serialVersionUID = -6056568239544794035L;

	@Override
	public void setupModule(SetupContext context) {
		this.setMixInAnnotation(ITerm.class, PolymorphicTermMixin.class);
		this.setMixInAnnotation(ILicenseTerm.class, PolymorphicTermMixin.class);
		this.setMixInAnnotation(IUsageTerm.class, PolymorphicTermMixin.class);
		super.setupModule(context);

		context.addBeanDeserializerModifier(new BeanDeserializerModifier() {
			public JsonDeserializer<?> modifyDeserializer(DeserializationConfig config, BeanDescription description, JsonDeserializer<?> deserializer) {
				if (ITerm.class.isAssignableFrom(description.getBeanClass())) return new StandardTermDeserializer(deserializer);
				if (ITerms.class.isAssignableFrom(description.getBeanClass())) return new TermsDeserializer();
				return deserializer;
			}
		});
		context.addBeanSerializerModifier(new BeanSerializerModifier() {
			@Override
			public JsonSerializer<?> modifySerializer(SerializationConfig config, BeanDescription description, JsonSerializer<?> serializer) {
				if (ITerm.class.isAssignableFrom(description.getBeanClass())) return new StandardTermSerializer();
				if (ITerms.class.isAssignableFrom(description.getBeanClass())) return new TermsSerializer();
				return serializer;
			}
		});
	}
}
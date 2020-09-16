package com.g2forge.reassert.reassert.convert.contract;

import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.DeserializationConfig;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializationConfig;
import com.fasterxml.jackson.databind.deser.BeanDeserializerModifier;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.BeanSerializerModifier;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.parser.IParser;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ContractModule extends SimpleModule {
	private static final long serialVersionUID = -6056568239544794035L;

	protected final IFunction1<? super Object, ? extends IDescription> describer;

	protected final IParser<ILicenseApplied> licenseParser;

	protected final IParser<IUsageApplied> usageParser;

	@Override
	public void setupModule(SetupContext context) {
		super.setupModule(context);

		context.addBeanDeserializerModifier(new BeanDeserializerModifier() {
			public JsonDeserializer<?> modifyDeserializer(DeserializationConfig config, BeanDescription description, JsonDeserializer<?> deserializer) {
				if (IVertex.class.isAssignableFrom(description.getBeanClass())) return new ContractDeserializer(deserializer, getLicenseParser(), getUsageParser());
				return deserializer;
			}
		});
		context.addBeanSerializerModifier(new BeanSerializerModifier() {
			@Override
			public JsonSerializer<?> modifySerializer(SerializationConfig config, BeanDescription description, JsonSerializer<?> serializer) {
				if (IVertex.class.isAssignableFrom(description.getBeanClass())) return new ContractSerializer(serializer, getDescriber());
				return serializer;
			}
		});
	}
}
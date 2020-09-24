package com.g2forge.reassert.reassert.convert.work;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.deser.ResolvableDeserializer;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.databind.jsontype.TypeDeserializer;
import com.g2forge.alexandria.java.function.IThrowSupplier;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.IWorkLicenseRule;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.RuleWorkType;
import com.g2forge.reassert.core.model.work.IWorkType;

import lombok.AccessLevel;
import lombok.Getter;

@Getter(AccessLevel.PROTECTED)
public class WorkTypeDeserializer extends StdDeserializer<IWorkType> implements ResolvableDeserializer {
	private static final long serialVersionUID = -2962075618799775838L;

	protected final JsonDeserializer<?> deserializer;

	protected WorkTypeDeserializer(JsonDeserializer<?> deserializer) {
		super(IWorkType.class);
		this.deserializer = deserializer;
	}

	@Override
	public IWorkType deserialize(JsonParser parser, DeserializationContext context) throws IOException, JsonProcessingException {
		return deserialize(parser, () -> getDeserializer().deserialize(parser, context));
	}

	protected IWorkType deserialize(JsonParser parser, IThrowSupplier<?, IOException> supplier) throws IOException {
		final ObjectCodec codec = parser.getCodec();
		final IWorkLicenseRule rule = codec.readValue(parser, IWorkLicenseRule.class);
		return new RuleWorkType(rule);
	}

	@Override
	public Object deserializeWithType(JsonParser parser, DeserializationContext context, TypeDeserializer typeDeserializer) throws IOException {
		return deserialize(parser, () -> super.deserializeWithType(parser, context, typeDeserializer));
	}

	@Override
	public void resolve(DeserializationContext context) throws JsonMappingException {
		final JsonDeserializer<?> deserializer = getDeserializer();
		if (deserializer instanceof ResolvableDeserializer) ((ResolvableDeserializer) deserializer).resolve(context);
	}
}
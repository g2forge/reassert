package com.g2forge.reassert.reassert.convert.contract;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.deser.ResolvableDeserializer;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.databind.jsontype.TypeDeserializer;
import com.g2forge.alexandria.java.core.error.HError;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IThrowSupplier;
import com.g2forge.reassert.core.api.parser.IParser;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.contract.usage.UnknownUsage;

import lombok.AccessLevel;
import lombok.Getter;

@Getter(AccessLevel.PROTECTED)
public class ContractDeserializer extends StdDeserializer<IVertex> implements ResolvableDeserializer {
	private static final long serialVersionUID = -6856117256176037393L;

	protected final JsonDeserializer<?> deserializer;

	protected final IParser<ILicenseApplied> licenseParser;

	protected final IParser<IUsageApplied> usageParser;

	protected ContractDeserializer(JsonDeserializer<?> deserializer, IParser<ILicenseApplied> licenseParser, IParser<IUsageApplied> usageParser) {
		super(IVertex.class);
		this.deserializer = deserializer;
		this.licenseParser = licenseParser;
		this.usageParser = usageParser;
	}

	@Override
	public IVertex deserialize(JsonParser parser, DeserializationContext context) throws IOException, JsonProcessingException {
		return (IVertex) deserialize(parser, () -> getDeserializer().deserialize(parser, context));
	}

	protected Object deserialize(JsonParser parser, IThrowSupplier<?, IOException> supplier) throws IOException {
		final JsonToken token = parser.currentToken();
		if (token == JsonToken.VALUE_STRING) return fromString(parser);
		return supplier.get();
	}

	@Override
	public Object deserializeWithType(JsonParser parser, DeserializationContext context, TypeDeserializer typeDeserializer) throws IOException {
		return deserialize(parser, () -> super.deserializeWithType(parser, context, typeDeserializer));
	}

	protected IVertex fromString(JsonParser parser) throws IOException {
		final String text = parser.getText().trim();
		return HError.apply((IFunction1<String, IVertex> valueOf) -> valueOf.apply(text), String.format("Could not parse \"%1$s\" as a contract", text), t -> {
			final ILicenseApplied parsed = getLicenseParser().parse(t);
			if (parsed instanceof UnknownLicense) throw new IllegalArgumentException();
			return parsed;
		}, t -> {
			final IUsageApplied parsed = getUsageParser().parse(t);
			if (parsed instanceof UnknownUsage) throw new IllegalArgumentException();
			return parsed;
		});
	}

	@Override
	public void resolve(DeserializationContext context) throws JsonMappingException {
		final JsonDeserializer<?> deserializer = getDeserializer();
		if (deserializer instanceof ResolvableDeserializer) ((ResolvableDeserializer) deserializer).resolve(context);
	}
}
package com.g2forge.reassert.reassert.convert.finding;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.jsontype.TypeSerializer;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.contract.model.TermConstant;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.contract.IContractTerms;

import lombok.Getter;

@Getter
public class TermConstantSerializer extends StdSerializer<TermConstant> {
	private static final long serialVersionUID = 4188997795853592234L;

	protected final IFunction1<? super IContractTerms, ? extends IDescription> describer;

	protected TermConstantSerializer(IFunction1<? super IContractTerms, ? extends IDescription> describer) {
		super(TermConstant.class);
		this.describer = describer;
	}

	@Override
	public void serialize(TermConstant value, JsonGenerator generator, SerializerProvider provider) throws IOException {
		generator.getCodec().writeValue(generator, toStored(false, value));
	}

	public void serializeWithType(TermConstant value, JsonGenerator generator, SerializerProvider provider, TypeSerializer typeSerializer) throws IOException {
		generator.getCodec().writeValue(generator, toStored(true, value));
	}

	protected StoredTermConstant toStored(boolean withType, TermConstant value) {
		final String contract = getDescriber().apply(value.getContract()).getName();
		return new StoredTermConstant(withType ? value.getClass().getName() : null, value.getTerm(), contract);
	}
}